import path from 'path';
import CssMinimizerPlugin from 'css-minimizer-webpack-plugin';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import TerserPlugin from "terser-webpack-plugin";
import webpack from 'webpack';
import ReplaceHashPlugin from './ReplaceHashPlugin.js';

export default {
    mode: 'production',
    entry: {
        im: {
            import: ['./loader/production/im.bundle.js', './src/Client/css/im.css'],
            dependOn: 'emoji'
        },
        login: './loader/production/login.bundle.js',
        profile: {
            import: ['./loader/production/profile.bundle.js', './src/Client/css/profile.css'],
            dependOn: 'im'
        },
        karmaPrivileges: {
            import: ['./loader/production/karmaPrivileges.bundle.js', './src/Client/css/karmaPrivileges.css'],
            dependOn: 'im'
        },
        help: ['./loader/production/help.bundle.js', './src/Client/css/help.css'],
        internalHelp: {
            import: './loader/production/internalHelp.bundle.js',
            dependOn: 'im'
        },
        settings: {
            import: ['./loader/production/settings.bundle.js', './src/Client/css/settings.css'],
            dependOn: 'im'
        },
        experiments: {
            import: ['./loader/production/experiments.bundle.js', './src/Client/css/experiments.css'],
            dependOn: 'im'
        },
        feedback: {
            import: ['./loader/production/feedback.bundle.js', './src/Client/css/feedback.css'],
            dependOn: 'im'
        },
        recover: './loader/production/recover.bundle.js',
        emoji: './output-es/Shared.Im.Emoji/index.js',
        base: './src/Client/css/base.css',
        external: './src/Client/css/external.css',
        backer: './src/Client/css/backer.css'
    },
    output: {
        path: path.resolve(".", 'dist/production'),
        filename: '[name].[contenthash].bundle.js'
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].[contenthash].css',
        }),
        new webpack.DefinePlugin({
            'process.env.PRODUCTION': true
        }),
        new ReplaceHashPlugin({ files: [{ dir: 'dist/production', prefix: 'common' }, 'output-es/Shared.Resource/index.js'] }),
    ],
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                    MiniCssExtractPlugin.loader,
                    {
                        loader: 'css-loader',
                        options: {
                            url: false,
                        }
                    }
                ]
            }
        ]
    },
    optimization: {
        removeEmptyChunks: true,
        moduleIds: 'deterministic',
        splitChunks: {
            chunks: 'all',
            name: 'common'
        },
        minimizer: [
            new TerserPlugin(),
            new CssMinimizerPlugin(),
        ]
    },
};
